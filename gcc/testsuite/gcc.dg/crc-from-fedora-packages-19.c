/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details  -O2" } */
/* { dg-require-effective-target int32plus } */

// File - liboctasic_la-oct6100_chip_open
// We don't support this case.

typedef unsigned int UINT32;
typedef UINT32 * PUINT32;
typedef struct _OCT6100_INSTANCE_API_
{

   // tPOCT6100_SHARED_INFO pSharedInfo;

    //PVOID pProcessContext;

    //tOCT6100_USER_SERIAL_OBJECT ulApiSerObj;


} tOCT6100_INSTANCE_API, *tPOCT6100_INSTANCE_API;

UINT32 Oct6100ApiProductionCrc(
    tPOCT6100_INSTANCE_API f_pApiInstance,
    PUINT32 f_pulMessage,
    UINT32 f_ulMessageLength,
    PUINT32 f_pulCrcResult )
{
  UINT32 ulWidth = 32;
  UINT32 ulKey, i, j;
  UINT32 ulRemainder = 0;


  ulRemainder = f_pulMessage[ f_ulMessageLength - 1 ];
  for ( j = f_ulMessageLength - 1; j != 0xFFFFFFFF ; j-- )
    {
      for ( i = 0; i < ulWidth; i++ )
	{
	  if ( ( ( ulRemainder >> 0x1F ) & 0x1 ) == 0x1 )
	    {
	      ulKey = 0x8765DCBA;
	    }
	  else
	    {
	      ulKey = 0;
	    }
	  ulRemainder = ulRemainder ^ ulKey;

	  ulRemainder = ulRemainder << 1;
	  if ( j != 0 )
	    {
	      ulRemainder = ulRemainder | ( ( f_pulMessage[ j - 1 ] ) >> ( 0x1F - i ) );
	    }
	}
    }

  *f_pulCrcResult = ulRemainder;

  return 0x00000000;
}
