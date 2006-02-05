// { dg-do run  }
// { dg-options "" }
class A {
public:
  operator bool () {
    return true;
  }
} a;
class A1 {
public:
  operator int () {
    return true;
  }
} a1;
class A2 {
public:
  operator const char * () {
    return "";
  }
} a2;
class A3 {
public:
  operator unsigned long long int () {
    return true;
  }
} a3;
class A4 {
public:
  operator const char * () {
    return "";
  }
  operator unsigned long long int () {
    return true;
  }
} a4;
class A5 {
public:
  operator double () {
    return 256.0e0*256.0e0*256.0e0*256.0e0*256.0e0*256.0e0*256.0e0*256.0e0
      *256.0e0*256.0e0*256.0e0*256.0e0*256.0e0*256.0e0*256.0e0*256.0e0
	*256.0e0*256.0e0*256.0e0*256.0e0*256.0e0*256.0e0*256.0e0*256.0e0
	  *256.0e0*256.0e0*256.0e0*256.0e0*256.0e0*256.0e0*256.0e0*256.0e0;
  }
} a5;
int i = true;
bool b = true;
bool c = (bool)(void (A::*)())0;
bool d = 256;
main() {
  if (!d) return 1;
  if (!a) return 1;
  if (!(bool)a) return 1;
  //  if (!(long long)a) return 1;
  if (!a1) return 1;
  if (!a2) return 1;
  if (!a3) return 1;
  if (!a5) return 1;
}
