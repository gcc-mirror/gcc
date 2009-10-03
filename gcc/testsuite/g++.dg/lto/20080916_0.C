/* { dg-lto-do assemble } */

enum _Ios_Fmtflags     {
 _S_boolalpha };

class ios_base   {
 static const  _Ios_Fmtflags boolalpha =   _S_boolalpha;
 _Ios_Fmtflags _M_flags;
};

ios_base& g() {
}
