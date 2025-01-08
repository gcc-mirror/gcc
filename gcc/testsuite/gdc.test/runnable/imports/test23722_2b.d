module imports.test23722_2b;

struct T(alias fun) { }

struct  S(int i) {
    auto t = T!(x => i)();
}

string g() {
    S!0 s0;
    S!1 s1;
    return s1.t.init.mangleof;
}
