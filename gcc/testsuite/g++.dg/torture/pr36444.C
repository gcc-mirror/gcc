/* { dg-do compile } */
#define vector __attribute__((vector_size(16) ))
struct struct1  {
  union {}    vmx;
  struct struct2   {
    struct2(const struct2& r) {}
  } w;
} __attribute__((aligned(16)));
struct struct3  {
  vector float vmx;
  operator const struct1& () const{
    return *reinterpret_cast<const struct1*>(this);
  }
};
struct3 func3( struct3 V1);
struct3 func2( void );
void func1( )  {
  struct1 vVec = func2() ;
  func3 ( (struct3&)vVec );
}

