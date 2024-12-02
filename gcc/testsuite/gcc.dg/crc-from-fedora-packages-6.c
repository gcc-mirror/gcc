/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */

// libmng_chunk_io
typedef unsigned int mng_uint32;
typedef signed int mng_int32;
typedef signed char mng_int8;
typedef mng_int8 mng_bool;
typedef struct mng_data_struct {
    // ...
    mng_uint32 aCRCtable [256];
    mng_bool bCRCcomputed;
    // ...
} mng_data;

typedef mng_data * mng_datap;
void make_crc_table (mng_datap pData)
{
    mng_uint32 iC;
    mng_int32 iN, iK;

    for (iN = 0; iN < 256; iN++)
    {
        iC = (mng_uint32) iN;

        for (iK = 0; iK < 8; iK++)
        {
            if (iC & 1)
                iC = 0xedb88320U ^ (iC >> 1);
            else
                iC = iC >> 1;
        }

        pData->aCRCtable [iN] = iC;
    }

    pData->bCRCcomputed = 1;
}

/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
