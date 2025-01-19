/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */
/* { dg-require-effective-target int32plus } */

//File - Ac3 from Fedora, polynomial made const. Original crc-side-instr-21.c test
#include <stddef.h>
typedef unsigned short int16u;
typedef unsigned char int8u;
int CRC16_Init(int16u *Table)
{
    for (size_t Pos=0; Pos<256; Pos++)
    {
        Table[Pos]=(int16u)Pos<<8;

        for(int8u bit=0; bit<8; bit++)
        {
            if (Table[Pos]&0x8000)
                Table[Pos]=(Table[Pos]<<1)^0x2101;
            else
                Table[Pos]=Table[Pos]<<1;
        }
    }
    return 0;
}

/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
