#ifndef CLONE_TEST_H
#define CLONE_TEST_H
// Potential fancy deep cloning test for use with unique_ptr

// disable by default
#if 0
#include <memory>

namespace clone_code {
    template<typename T>
    class abstract_method {};

    template<typename T>
    class virtual_inherit_from : virtual public T {
        using T::T;
    };

    template<typename Derived, typename... Bases>
    class clone_inherit : public Bases... {
      public:
        virtual ~clone_inherit() = default;

        std::unique_ptr<Derived> clone() const {
            return std::unique_ptr<Derived>(static_cast<Derived*>(this->clone_impl()));
        }

      protected:
      private:
        virtual clone_inherit* clone_impl() const override {
            return new Derived(static_cast<const Derived&>(*this));
        }
    };

    template<typename Derived, typename... Bases>
    class clone_inherit<abstract_method<Derived>, Bases...> : public Bases... {
      public:
        virtual ~clone_inherit() = default;

        std::unique_ptr<Derived> clone() const {
            return std::unique_ptr<Derived>(static_cast<Derived*>(this->clone_impl()));
        }

      protected:
      private:
        virtual clone_inherit* clone_impl() const = 0;
    };

    template<typename Derived>
    class clone_inherit<Derived> {
      public:
        virtual ~clone_inherit() = default;

        std::unique_ptr<Derived> clone() const {
            return std::unique_ptr<Derived>(static_cast<Derived*>(this->clone_impl()));
        }

      private:
        virtual clone_inherit* clone_impl() const override {
            return new Derived(static_cast<const Derived&>(*this));
        }
    };

    template<typename Derived>
    class clone_inherit<abstract_method<Derived> > {
      public:
        virtual ~clone_inherit() = default;

        std::unique_ptr<Derived> clone() const {
            return std::unique_ptr<Derived>(static_cast<Derived*>(this->clone_impl()));
        }

      private:
        virtual clone_inherit* clone_impl() const = 0;
    };
}

namespace user_code {
    using namespace clone_code;

    class cloneable : public clone_inherit<abstract_method<cloneable> > {};

    class foo : public clone_inherit<abstract_method<foo>, virtual_inherit_from<cloneable> > {};

    class bar : public clone_inherit<abstract_method<bar>, virtual_inherit_from<cloneable> > {};

    class concrete : public clone_inherit<concrete, foo, bar> {};
}
#endif

#endif
