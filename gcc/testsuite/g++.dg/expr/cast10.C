// { dg-do compile }
// This used to error out because we would try to convert m to a short.


struct a {};
void b() {
    int a::*m;
    a *c;
    short p = reinterpret_cast<char*>(&(c->*m)) - reinterpret_cast<char*>(c);
}
