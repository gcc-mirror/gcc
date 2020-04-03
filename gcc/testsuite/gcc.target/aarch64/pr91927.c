/* { dg-do compile } */
/* { dg-options "-mstrict-align -O3" } */

#define NULL 0

typedef unsigned uint32_t;
typedef struct __attribute__((__packed__))
{
	uint32_t nTagID;
	uint32_t nValueBufferSize;
	uint32_t nValueLength;
	
}	PropertyTags_t;

typedef struct
{
	char *szName;
	uint32_t nBufferSize;
	uint32_t nLength;
	
}	Something_t;

void SetTag(PropertyTags_t *pTag, uint32_t nBufferSize, uint32_t nLength);

void TestCase(Something_t *pSome, uint32_t nBufferSize, uint32_t nLength)
{
	if (pSome != NULL)
	{
		PropertyTags_t sTag = { 0 };
		
		SetTag(&sTag, nBufferSize, nLength);
		
		pSome->nBufferSize = sTag.nValueBufferSize;
		pSome->nLength = sTag.nValueLength;
	}
}

/* { dg-final { scan-assembler-not "ldr\td" } } */
