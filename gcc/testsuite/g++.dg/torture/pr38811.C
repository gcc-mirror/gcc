/* { dg-do compile } */

typedef unsigned long ULONG;
void iwos_ErrorMessage(long error, const char * const file_name,
                       ULONG line_num, const char * const message);
class AbcA2d {
public:
    double x;
    double y;
    ~AbcA2d() { }
};
enum AbcZyParamType { ABC_SP_1 };
class AbcExtent2d {
    AbcA2d m_vMin;
    AbcA2d m_vMax;
public:
    AbcExtent2d(const AbcA2d & rMin, const AbcA2d & rMax);
    AbcA2d ClampPoint2d(const AbcA2d & rPoint) const;
    AbcA2d GetMax() const { return m_vMax; }
    AbcA2d GetMin() const { return AbcA2d(); }
    AbcA2d Evaluate(double dNormalizedX, double dNormalizedY) const;
};
inline AbcExtent2d::AbcExtent2d(const AbcA2d & rMin, const AbcA2d & rMax)
{
    if (rMin.x > rMax.x || rMin.y > rMax.y)
      {
        long sErr = (1007);
        if (sErr != 1000)
          iwos_ErrorMessage(sErr,(const char * const)__null,
                            0,(const char * const)__null);
      }
    else
      {
        m_vMin = rMin;
        m_vMax = rMax;
      }
}
inline AbcA2d AbcExtent2d::ClampPoint2d(const AbcA2d & rPoint) const
{
    AbcA2d sRet = rPoint;
    if (rPoint.x < m_vMin.x)
      sRet.x = m_vMin.x;
    return sRet;
}
inline AbcA2d AbcExtent2d::Evaluate(double dNormalizedX, double dNormalizedY)
const
{
    AbcA2d sRet;
    sRet.x = m_vMin.x + dNormalizedX * (m_vMax.x - m_vMin.x);
    sRet.y = m_vMin.y + dNormalizedY * (m_vMax.y - m_vMin.y);
    return ClampPoint2d(sRet);
}
class AbcAbcdTracer {
    AbcExtent2d m_vUVDomain;
    virtual long TestIsoAbcde(AbcZyParamType eZyParam, double dParam,
                              int & rbZyxIsSolution);
    virtual int DoesPointLieOnAbcde(AbcA2d & rUV, int bRefinePoint) const;
};
long AbcAbcdTracer::TestIsoAbcde(AbcZyParamType eZyParam, double dParam,
                                 int & rbZyxIsSolution)
{
    AbcA2d sUV1(m_vUVDomain.GetMin());
    AbcA2d sUV2(m_vUVDomain.GetMax());
    AbcExtent2d sUVIso(sUV1,sUV2);
    for (ULONG i=0; i<10; i++)
      {
        double dT = i / (10 -1.0);
        AbcA2d sUV = sUVIso.Evaluate(dT,dT);
        if (!DoesPointLieOnAbcde(sUV,0))
          ;
      }

    return 0;
}

