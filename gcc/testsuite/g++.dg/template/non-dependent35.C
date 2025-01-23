// PR c++/117925

typedef int(*FnPtr)();

template<class T>
void fnICE(void* fnPtr) {
  ((FnPtr)fnPtr)();
}
