// PR c++/58083
// { dg-do compile { target c++11 } }

namespace details {
struct iterator_concept_checker
{
    typedef char yes_type;
    typedef char (&no_type)[2];

    template <typename T>
    static no_type test(...);

    template <typename T>
    static yes_type test(
        int*
	, void (*)(T) = [](T it)
        {
            auto copy = T{it};                              // copy constructible
            copy = it;                                      // copy assignable
            copy.~T();                                      // destroyable
            ++it;                                           // incrementable
        }
      );
};
}

int main()
{
  details::iterator_concept_checker::test<int>(0);
}
