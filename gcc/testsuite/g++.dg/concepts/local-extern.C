// { dg-do compile { target c++17 } }
// { dg-additional-options -fconcepts }

// Don't attach constraints to block-scope fn-decls and ICE

template<typename _Iter>
    concept input_or_output_iterator
      = requires(_Iter __i) { { *__i } ; };


  template<input_or_output_iterator _It>
  class common_iterator
  {

  public:
    
void
      frob ()
    {
      if (__builtin_is_constant_evaluated())
	{
	  void __failed_assertion(); // ICEd
	  if (!bool(_M_index == 0)) __failed_assertion();
	}

    }

  private:
    unsigned char _M_index;
  };

template <typename T> concept C = true;

template<typename T>
void F ()
{
  void bad () requires C<T>; // { dg-error "a non-templated function" }
  
}
