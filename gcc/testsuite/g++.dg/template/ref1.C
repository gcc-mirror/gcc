class a {} a1;
template <a & p> class b { public: b() { static_cast <a &> (p); }; };
int main() { b <a1> b1; }
