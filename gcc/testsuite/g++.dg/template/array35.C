// PR c++/104302

struct ss {};
static ss ff(void* const v);
template <unsigned mem_size>
void f1(void) {
    int mem[mem_size];
    ss StateRegs[] = {
        ff(mem)
    };
}
