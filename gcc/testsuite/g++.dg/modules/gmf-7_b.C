// { dg-additional-options "-fmodules" }
import M;
extern "C" int scanf(const char *, ...);
int scanf(const char *, ...);
using ::scanf;
