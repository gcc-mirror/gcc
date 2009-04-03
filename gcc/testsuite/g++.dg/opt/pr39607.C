// PR rtl-optimization/39607
// { dg-do compile }
// { dg-options "-O2" }

void abcErrorMessage(int error);
enum AbcSurfType {
    SURF_U,
    SURF_V
};
class AbcVec2d
{
public:
    double x;
    double y;
};
class AbcIval1d
{
protected:
    double m_dMin;
    double m_dMax;
public:
    AbcIval1d();
    AbcIval1d(double dMin, double dMax);
    double GetMax() const { return m_dMax; }
    double GetMin() const { return m_dMin; }
};
inline AbcIval1d::AbcIval1d(double dMin, double dMax)
{
    if (dMin > dMax) {
        abcErrorMessage(1);
    }
    else {
        m_dMin = dMin;
        m_dMax = dMax;
    }
}
class AbcIval2d
{
protected:
    AbcVec2d m_vMin;
    AbcVec2d m_vMax;
public:
    AbcVec2d GetMax() const { return m_vMax; }
    AbcVec2d GetMin() const { return m_vMin; }
};
class AbcCone
{
protected:
    int m_uv;    	
public:
    AbcIval2d GetNaturalUVDomain() const;
    AbcIval1d GetLinearParamIval(AbcSurfType * pSurfParam) const;
};
AbcIval1d AbcCone::GetLinearParamIval(AbcSurfType * pSurfParam) const
{
    AbcIval1d sIval;
    AbcIval2d sUVDomain = GetNaturalUVDomain();
    if (m_uv) {    
        sIval = AbcIval1d(sUVDomain.GetMin().x,sUVDomain.GetMax().x);
    }
    else { 
        sIval = AbcIval1d(sUVDomain.GetMin().y,sUVDomain.GetMax().y);
    }
    return sIval;
}
