// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }

// https://gcc.gnu.org/PR108620
#include <iostream>
#include <memory>
#include <coroutine>

template<class PrivateDataType>
struct task;

template <class PrivateDataType>
struct task_private_data {
  inline task_private_data() noexcept : data_(nullptr) {}
  inline task_private_data(PrivateDataType* input) noexcept : data_(input) {}
  inline task_private_data(task_private_data&& other) noexcept = default;
  inline task_private_data& operator=(task_private_data&&) noexcept = default;
  inline task_private_data(const task_private_data&) = delete;
  inline task_private_data& operator=(const task_private_data&) = delete;
  inline ~task_private_data() {}

  inline bool await_ready() const noexcept { return true; }
  inline PrivateDataType* await_resume() const noexcept { return data_; }
  inline void await_suspend(std::coroutine_handle<>) noexcept {}

  PrivateDataType* data_;
};

template<class PrivateDataType>
struct task_context {
    PrivateDataType data_;
};

template<class PrivateDataType>
struct task {
    using self_type = task<PrivateDataType>;
    std::shared_ptr<task_context<PrivateDataType>> context_;

    task(const std::shared_ptr<task_context<PrivateDataType>>& input): context_(input) {}

    static auto yield_private_data() noexcept { return task_private_data<PrivateDataType>{}; }

    struct promise_type {
      std::shared_ptr<task_context<PrivateDataType>> context_;

      template<class Input, class... Rest>
      promise_type(Input&& input, Rest&&...) {
        context_ = std::make_shared<task_context<PrivateDataType>>();
        context_->data_ = std::forward<Input>(input);
      }

      auto get_return_object() noexcept { return self_type{context_}; }
      std::suspend_never initial_suspend() noexcept { return {}; }
      std::suspend_never final_suspend() noexcept { return {}; }
      void unhandled_exception() { throw; }

      template<class ReturnType>
      void return_value(ReturnType&&) {}

      template <class InputPrivateDataType>
      inline task_private_data<InputPrivateDataType> yield_value(
          task_private_data<InputPrivateDataType>&& input) noexcept {
        input.data_ = &context_->data_;
        return task_private_data<InputPrivateDataType>(input.data_);
      }
    };
};

template<class TArg, class OutputType>
task<std::string> call1(TArg&& arg, OutputType& output) {
    OutputType* ptr = co_yield task<TArg>::yield_private_data();
    output = *ptr;
    co_return 0;
}


struct container {
    std::string* ptr;
};

template<class TArg>
task<std::string> call2(TArg&& arg, container& output) {
    output.ptr = co_yield task<TArg>::yield_private_data();
    co_return 0;
}

int main() {
  // success
  std::string output1;
  call1(std::string("hello1"), output1);
  std::cout<< "output1: "<< output1<< std::endl;

  // crash
  container output2;
  auto task2 = call2(std::string("hello2"), output2);
  std::cout<< "output2: "<< *output2.ptr<< std::endl;
  return 0;
}
