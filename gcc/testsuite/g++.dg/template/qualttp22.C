// { dg-do compile }

// Origin: Philippe Van Deyck <hetadres@email.com>

// PR c++/13520: Default template template argument that is a qualified id
// with dependent scope.

template<typename regular_type> class Policy {};

template <typename regular_type, template<typename> class OriginalPolicy>
class ChangedPolicy_impl {};

template <template<typename> class OriginalPolicy > class ChangedPolicy {
public:
  template<typename regular_type> class Type : public 
  ChangedPolicy_impl<regular_type,OriginalPolicy> { };
};

template <typename regular_type, template<typename> class Policy1,
	  template<typename> class Policy2
	    = ChangedPolicy<Policy1>::template Type>
class Host : public Policy1<regular_type>, public Policy2<regular_type> { };

int main()
{
  Host<void, Policy> h;
  return 0;
}
