// { dg-do assemble  }

struct X
{ 
  static const bool   is_signed  =  true  ;
  static const int digits = is_signed ? 8 *sizeof(wchar_t)-1 : 0;
};
