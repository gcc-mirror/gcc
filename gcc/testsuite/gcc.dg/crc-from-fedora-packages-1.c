/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */

// File_Ac3.cpp.ii
// We don't support non-constant polynomials.

#include <stddef.h>
typedef unsigned short int16u;
typedef unsigned char int8u;
int CRC16_Init(int16u *Table, int16u Polynomial)
{
    for (size_t Pos=0; Pos<256; Pos++)
    {
        Table[Pos]=(int16u)Pos<<8;

        for(int8u bit=0; bit<8; bit++)
        {
            if (Table[Pos]&0x8000)
                Table[Pos]=(Table[Pos]<<1)^Polynomial;
            else
                Table[Pos]=Table[Pos]<<1;
        }
    }
    return 0;
}

/* { dg-final { scan-tree-dump "Second operand of the xor statement isn't an integer constant.\n" "crc" } } */
