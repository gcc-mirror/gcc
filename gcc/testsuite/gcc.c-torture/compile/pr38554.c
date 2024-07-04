typedef unsigned long sample;
struct pam
{
  sample maxval;
};
typedef sample *tuple;
enum function
  {
    FN_MULTIPLY, FN_DIVIDE, FN_ADD, FN_SUBTRACT, FN_MIN, FN_MAX, FN_AND, FN_OR,
    FN_XOR, FN_NOT, FN_SHIFTLEFT, FN_SHIFTRIGHT
  };
struct cmdlineInfo
{
  enum function function;
  union
  {
    float divisor;
    unsigned int shiftCount;
  }
    u;
};
void
applyFunction (struct cmdlineInfo const cmdline, struct pam const inpam,
               struct pam const outpam, tuple * const inputRow,
               tuple * const outputRow)
{
  float const oneOverDivisor = 1 / cmdline.u.divisor;
  int col;
  {
    int plane;
    {
      sample const inSample = inputRow[col][plane];
      sample outSample;
      switch (cmdline.function)
        {
        case FN_DIVIDE:
          outSample = ((unsigned int) ((inSample * oneOverDivisor) + 0.5));
          break;
        case FN_SHIFTLEFT:
          outSample = (inSample << cmdline.u.shiftCount) & outpam.maxval;
        }
      outputRow[col][plane] =
        ((outpam.maxval) < (outSample) ? (outpam.maxval) : (outSample));
    }
  }
}
