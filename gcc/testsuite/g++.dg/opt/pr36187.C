/* { dg-do run } */
/* { dg-options "-O2" } */

extern "C" void abort (void);
enum SbxDataType { SbxINTEGER, SbxDECIMAL, SbxBYREF = 0x4000 };
struct SbxValues {
    union {
	float nSingle;
	float* pSingle;
    };
    SbxDataType eType;
};
static bool ImpPutDoubleFoo( SbxValues* p)
{
    bool bRet = false;
    SbxValues aTmp;
    int count = 0;
start: 
    switch( p->eType )  {  
	case SbxINTEGER:
	    if (count++ > 0)
	      abort ();
	    aTmp.pSingle = &p->nSingle; goto direct;
	case SbxBYREF | SbxDECIMAL:
	    bRet = false;
	    break;
	direct:
	    aTmp.eType = SbxDataType( p->eType | SbxBYREF );
	    p = &aTmp; goto start;
	case SbxBYREF | SbxINTEGER:
	    break;
	default:
	    bRet =true;
    }
    return bRet;
}

int main( int argc, char** argv )
{
    SbxValues aTmp;
    aTmp.eType = SbxINTEGER;
    if ( ImpPutDoubleFoo( &aTmp ) )
	abort ();
    return 0;
}
