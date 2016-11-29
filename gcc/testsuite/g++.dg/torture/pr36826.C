#if __cplusplus > 201402L
template <class T> T CoinMax(const T x1, const T x2); 
template <class T> T CoinMin(const T x1, const T x2);
#else
template <class T> T CoinMax(register const T x1, register const T x2); 
template <class T> T CoinMin(register const T x1, register const T x2);
#endif
class CoinIndexedVector;
class ClpModel {
protected:
    double objectiveScale_;
    double rhsScale_;
    int numberRows_;
    int numberColumns_;
    double * rowActivity_;
    double * columnActivity_;
    double * dual_;
    double * reducedCost_;
    double* rowLower_;
    double* rowUpper_;
    double * rowObjective_;
    double * columnLower_;
    double * columnUpper_;
    double * rowScale_;
    double * columnScale_;
    double * inverseRowScale_;
    double * inverseColumnScale_;
    int problemStatus_;
    int secondaryStatus_;
};
class ClpSimplex : public ClpModel {
    void deleteRim(int getRidOfFactorizationData=2);
    double upperOut_;
    double dualTolerance_;
    double primalTolerance_;
    double * rowLowerWork_;
    double * columnLowerWork_;
    double * rowUpperWork_;
    double * columnUpperWork_;
    double * rowObjectiveWork_;
    CoinIndexedVector * columnArray_[6];
    double * reducedCostWork_;
    double * rowActivityWork_;
    double * columnActivityWork_;
    ClpSimplex * auxiliaryModel_;
};
class CoinIndexedVector {
public:
    void clear();
};
void ClpSimplex::deleteRim(int getRidOfFactorizationData)
{
  int numberRows=numberRows_;
  int numberColumns=numberColumns_;
  int i;
  int numberPrimalScaled=0;
  int numberPrimalUnscaled=0;
  int numberDualScaled=0;
  int numberDualUnscaled=0;
  double scaleC = 1.0/objectiveScale_;
  double scaleR = 1.0/rhsScale_;
  if (!inverseColumnScale_) {
      for (i=0; i<numberColumns; i++)
	{
	  double scaleFactor = columnScale_[i];
	  double valueScaled = columnActivityWork_[i];
	  double lowerScaled = columnLowerWork_[i];
	  double upperScaled = columnUpperWork_[i];
	  if (lowerScaled>-1.0e20||upperScaled<1.0e20) {
	      if (valueScaled<lowerScaled-primalTolerance_||   valueScaled>upperScaled+primalTolerance_)
		numberPrimalScaled++;
	      else
		upperOut_ = CoinMax(upperOut_,CoinMin(valueScaled-lowerScaled,upperScaled-valueScaled));
	  }
	  columnActivity_[i] = valueScaled*scaleFactor*scaleR;
	  double value = columnActivity_[i];
	  if (value<columnLower_[i]-primalTolerance_)
	    numberPrimalUnscaled++;
	  else if (value>columnUpper_[i]+primalTolerance_)
	    numberPrimalUnscaled++;
	  double valueScaledDual = reducedCostWork_[i];
	  if (valueScaled>columnLowerWork_[i]+primalTolerance_&&valueScaledDual>dualTolerance_)
	    numberDualScaled++;
	  if (valueScaled<columnUpperWork_[i]-primalTolerance_&&valueScaledDual<-dualTolerance_)
	    numberDualScaled++;
	  reducedCost_[i] = (valueScaledDual*scaleC)/scaleFactor;
	  double valueDual = reducedCost_[i];
	  if (value>columnLower_[i]+primalTolerance_&&valueDual>dualTolerance_)
	    numberDualUnscaled++;
	  if (value<columnUpper_[i]-primalTolerance_&&valueDual<-dualTolerance_)
	    numberDualUnscaled++;
	}
      for (i=0; i<numberRows; i++)
	{
	  double scaleFactor = rowScale_[i];
	  double valueScaled = rowActivityWork_[i];
	  double lowerScaled = rowLowerWork_[i];
	  double upperScaled = rowUpperWork_[i];
	  if (lowerScaled>-1.0e20||upperScaled<1.0e20) {      if (valueScaled<lowerScaled-primalTolerance_||   valueScaled>upperScaled+primalTolerance_)        numberPrimalScaled++;      else        upperOut_ = CoinMax(upperOut_,CoinMin(valueScaled-lowerScaled,upperScaled-valueScaled));    }
	  rowActivity_[i] = (valueScaled*scaleR)/scaleFactor;
	  double value = rowActivity_[i];
	  if (value<rowLower_[i]-primalTolerance_)      numberPrimalUnscaled++;
	  else if (value>rowUpper_[i]+primalTolerance_)      numberPrimalUnscaled++;
	  double valueScaledDual = dual_[i]+rowObjectiveWork_[i];
	  ;
	  if (valueScaled>rowLowerWork_[i]+primalTolerance_&&valueScaledDual>dualTolerance_)      numberDualScaled++;
	  if (valueScaled<rowUpperWork_[i]-primalTolerance_&&valueScaledDual<-dualTolerance_)      numberDualScaled++;
	  dual_[i] *= scaleFactor*scaleC;
	  double valueDual = dual_[i];
	  if (rowObjective_)      valueDual += rowObjective_[i];
	  if (value>rowLower_[i]+primalTolerance_&&valueDual>dualTolerance_)      numberDualUnscaled++;
	  if (value<rowUpper_[i]-primalTolerance_&&valueDual<-dualTolerance_)      numberDualUnscaled++;
	}
  }
  const double * inverseScale = inverseColumnScale_;
  for (i=0; i<numberColumns; i++)
    {
      double scaleFactor = columnScale_[i];
      double valueScaled = columnActivityWork_[i];
      double lowerScaled = columnLowerWork_[i];
      double upperScaled = columnUpperWork_[i];
      if (lowerScaled>-1.0e20||upperScaled<1.0e20) {      if (valueScaled<lowerScaled-primalTolerance_||   valueScaled>upperScaled+primalTolerance_)        numberPrimalScaled++;      else        upperOut_ = CoinMax(upperOut_,CoinMin(valueScaled-lowerScaled,upperScaled-valueScaled));    }
      columnActivity_[i] = valueScaled*scaleFactor*scaleR;
      double value = columnActivity_[i];
      if (value<columnLower_[i]-primalTolerance_)      numberPrimalUnscaled++;
      else if (value>columnUpper_[i]+primalTolerance_)      numberPrimalUnscaled++;
      double valueScaledDual = reducedCostWork_[i];
      if (valueScaled>columnLowerWork_[i]+primalTolerance_&&valueScaledDual>dualTolerance_)      numberDualScaled++;
      if (valueScaled<columnUpperWork_[i]-primalTolerance_&&valueScaledDual<-dualTolerance_)      numberDualScaled++;
      reducedCost_[i] = (valueScaledDual*scaleC)*inverseScale[i];
      double valueDual = reducedCost_[i];
      if (value>columnLower_[i]+primalTolerance_&&valueDual>dualTolerance_)      numberDualUnscaled++;
      if (value<columnUpper_[i]-primalTolerance_&&valueDual<-dualTolerance_)      numberDualUnscaled++;
    }
  inverseScale = inverseRowScale_;
  for (i=0; i<numberRows; i++)
    {
      double scaleFactor = rowScale_[i];
      double valueScaled = rowActivityWork_[i];
      double lowerScaled = rowLowerWork_[i];
      double upperScaled = rowUpperWork_[i];
      if (lowerScaled>-1.0e20||upperScaled<1.0e20) {      if (valueScaled<lowerScaled-primalTolerance_||   valueScaled>upperScaled+primalTolerance_)        numberPrimalScaled++;      else        upperOut_ = CoinMax(upperOut_,CoinMin(valueScaled-lowerScaled,upperScaled-valueScaled));    }
      rowActivity_[i] = (valueScaled*scaleR)*inverseScale[i];
      double value = rowActivity_[i];
      if (value<rowLower_[i]-primalTolerance_)      numberPrimalUnscaled++;
      else if (value>rowUpper_[i]+primalTolerance_)      numberPrimalUnscaled++;
      double valueScaledDual = dual_[i]+rowObjectiveWork_[i];
      ;
      if (valueScaled>rowLowerWork_[i]+primalTolerance_&&valueScaledDual>dualTolerance_)      numberDualScaled++;
      if (valueScaled<rowUpperWork_[i]-primalTolerance_&&valueScaledDual<-dualTolerance_)      numberDualScaled++;
      dual_[i] *= scaleFactor*scaleC;
      double valueDual = dual_[i];
      if (rowObjective_)      valueDual += rowObjective_[i];
      if (value>rowLower_[i]+primalTolerance_&&valueDual>dualTolerance_)      numberDualUnscaled++;
      if (value<rowUpper_[i]-primalTolerance_&&valueDual<-dualTolerance_)      numberDualUnscaled++;
    }
  if (numberPrimalUnscaled) {
      if (numberDualUnscaled) 
	secondaryStatus_=4;
      else
	secondaryStatus_=2;
  }
  if (numberDualUnscaled)
    secondaryStatus_=3;
  int iRow,iColumn;
  for (iRow=0; iRow<4; iRow++)
    ;
  for (iColumn=0; iColumn<2; iColumn++)
    if (columnArray_[iColumn])
      columnArray_[iColumn]->clear();
}
