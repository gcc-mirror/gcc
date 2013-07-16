// PR c++/55879
// { dg-do compile { target c++11 } }

class CAddress
{
public:
  constexpr CAddress(unsigned long begin) : m_Begin(begin) {}
  constexpr CAddress(const CAddress &other) : m_Begin(other.m_Begin) {}

private:
  unsigned long m_Begin;
};

extern "C" char _lnkDDRRAM;
/* internal compiler error on gcc 4.6.3 */
const CAddress s_Memmap[2]
{
  {(unsigned long)&_lnkDDRRAM}, /* segmentation fault */
  {0x40000000},
};

class CNested {
public:
  constexpr CNested(const CAddress primary)
    : m_PrimaryBlock(primary) {}

private:
  CAddress m_PrimaryBlock;
};

/* internal compiler error on gcc 4.7.2 */
const CNested s_taskDescriptions[2]
{
  {{0x42000000}},
  {{0x43000000}},
};
