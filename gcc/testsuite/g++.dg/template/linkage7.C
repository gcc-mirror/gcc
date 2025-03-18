// PR c++/119194
// { dg-do compile { target c++11 } }

template <const int& Str>
[[gnu::noipa]]
int get_length() {
    return Str;
}
static constexpr int sssss{ 3};
int main() {
   if (get_length<sssss>() != sssss)
     __builtin_abort();
    return 0;
}

// { dg-final { scan-assembler {_Z10get_lengthIL_ZL5sssssEEiv} } }
// { dg-final { scan-assembler-not {(weak|glob)[^\n]*_Z10get_lengthIL_Z5sssssEEiv} } }
