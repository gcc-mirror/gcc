// { dg-do assemble  }

template <typename _CharT>
  struct moneypunct
{
   moneypunct (); 
};

template <>
  moneypunct<char>::moneypunct ();
