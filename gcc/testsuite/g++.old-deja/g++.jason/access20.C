// { dg-do assemble  }
// PRMS Id: 6662
// Bug: we crash trying to check the access on anglesSinesCosines.

#define Double double

class GCTransform
{
  protected:
    struct angle
    {
	Double phi1 ;
	Double phi2 ;
    } ;

    struct sineAndCosine
    {
	Double cosine1 ;
	Double cosine2 ;
	Double sine1 ;
	Double sine2 ;
    } ;

    union anglesSinesCosines
    {
	struct angle a ;
	struct sineAndCosine siCo ;
    } ;
};

class GCTransTransmit : public GCTransform
{
  protected:

    struct GCTransTransmitDataTemp
    {
	union anglesSinesCosines t ; // causes abort
    } ;
} ;
