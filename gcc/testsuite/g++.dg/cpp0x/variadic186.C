// PR c++/111592
// { dg-do compile { target c++11 } }

struct ignore { ignore(...) {} };

template<class... Args>
void InternalCompilerError(Args... args)
{ ignore{ ignore(args) ... }; }

int main()
{ InternalCompilerError(0, 0); }
