/* { dg-do compile } */

/* Check if entry points get implicit C linkage. If they don't, compiler will
 * error on incompatible declarations */

int main();
extern "C" int main();

#ifdef __MINGW32__

int wmain();
extern "C" int wmain();

int DllMain();
extern "C" int DllMain();

int WinMain();
extern "C" int WinMain();

int wWinMain();
extern "C" int wWinMain();

#endif

