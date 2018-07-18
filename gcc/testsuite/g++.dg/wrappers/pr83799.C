struct DataLayout;
struct TargetLoweringBase {
  void getTypeLegalizationCost(const DataLayout &DL) const;
};
struct TargetTransformInfoImplBase {
  const DataLayout &DL;
};
template <typename T>
struct TargetTransformInfoImplCRTPBase : public TargetTransformInfoImplBase {};
template <typename T>
struct BasicTTIImplBase : public TargetTransformInfoImplCRTPBase<T> {
  const TargetLoweringBase *getTLI() const;
  using TargetTransformInfoImplBase::DL;
  void getArithmeticInstrCost() {
    const TargetLoweringBase *TLI = getTLI();
    TLI->getTypeLegalizationCost(DL);
  }
};
