// Build don't link:

template <typename _CharT>
  struct moneypunct
{
   moneypunct (); 
};

template <>
  moneypunct<char>::moneypunct ();
