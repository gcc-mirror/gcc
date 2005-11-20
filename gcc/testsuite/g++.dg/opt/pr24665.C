// { dg-do compile }
// { dg-options "-O2" }

typedef unsigned long T;
typedef volatile T* const hwreg_t;
struct RegisterLayout
{
    T intmask;
};
struct Controller_t
{
    Controller_t();
    inline void
    disableInterrupt()
    {
        *mpMaskRegister = 0;
    };
    static hwreg_t mpMaskRegister;
};

extern char SimulatedRegisters[];

hwreg_t Controller_t::mpMaskRegister
  = &(reinterpret_cast<volatile RegisterLayout*>(SimulatedRegisters))->intmask;

Controller_t::Controller_t()
{
    disableInterrupt();
}
