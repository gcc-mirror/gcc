// PR 11767
// { dg-do run }
// { dg-require-profiling "-fprofile-arcs" }
// { dg-options "-fnon-call-exceptions -fprofile-arcs" }

#include <string>

typedef unsigned long ACE_UINT32;
extern "C" void abort();

static ACE_UINT32 const msc_maxCurrencyID = 9999;

class ResourceBalanceType2
{
  public:
   explicit ResourceBalanceType2(
      ACE_UINT32 resourceBalanceTypeID,
      ACE_UINT32 isoValue,
      const std::string& rc_shortName,
      const std::string& rc_symbol
    );
  public:
    const ACE_UINT32 mc_resBalTypeID;
    const ACE_UINT32 mc_isoValue;
    const std::string mc_shortName;
    const std::string mc_symbol;
};

void f(){}

ResourceBalanceType2::ResourceBalanceType2(
    ACE_UINT32 resourceBalanceTypeID,
    ACE_UINT32 isoValue,
    const std::string& rc_shortName,
    const std::string& rc_symbol)
  : mc_resBalTypeID(resourceBalanceTypeID),
    mc_isoValue(isoValue),
    mc_shortName(rc_shortName),
    mc_symbol(rc_symbol)
{
  bool isGreater = (mc_isoValue > msc_maxCurrencyID);
  f();
  bool temp = mc_isoValue > msc_maxCurrencyID;
  if (!isGreater) abort();
  if (!temp) abort();
}

int main (int argc, char * argv[])
{
  ACE_UINT32 const mc_isoValue = 10000;
  ResourceBalanceType2 rbResourceBalanceType2(3, mc_isoValue, "ATM", "M");
}

// { dg-final { cleanup-coverage-files } }
