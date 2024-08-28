/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-switchconv -mbmi" } */

/* Checks that the exponential index transformation is done for all these types
   of the index variable:
   - (unsigned) char
   - (unsigned) short
   - (unsigned) int
   - (unsigned) long
   - (unsigned) long long  */

int unopt_char(char bit_position)
{
    switch (bit_position)
    {
        case (1 << 0):
            return 0;
        case (1 << 1):
            return 1;
        case (1 << 2):
            return 2;
        case (1 << 3):
            return 3;
        case (1 << 4):
            return 4;
        case (1 << 5):
            return 5;
        case (1 << 6):
            return 6;
        default:
            return 0;
    }
}

int unopt_unsigned_char(unsigned char bit_position)
{
    switch (bit_position)
    {
        case (1 << 0):
            return 0;
        case (1 << 1):
            return 1;
        case (1 << 2):
            return 2;
        case (1 << 3):
            return 3;
        case (1 << 4):
            return 4;
        case (1 << 5):
            return 5;
        case (1 << 6):
            return 6;
        default:
            return 0;
    }
}

int unopt_short(short bit_position)
{
    switch (bit_position)
    {
        case (1 << 0):
            return 0;
        case (1 << 1):
            return 1;
        case (1 << 2):
            return 2;
        case (1 << 3):
            return 3;
        case (1 << 4):
            return 4;
        case (1 << 5):
            return 5;
        case (1 << 6):
            return 6;
        default:
            return 0;
    }
}

int unopt_unsigned_short(unsigned short bit_position)
{
    switch (bit_position)
    {
        case (1 << 0):
            return 0;
        case (1 << 1):
            return 1;
        case (1 << 2):
            return 2;
        case (1 << 3):
            return 3;
        case (1 << 4):
            return 4;
        case (1 << 5):
            return 5;
        case (1 << 6):
            return 6;
        default:
            return 0;
    }
}

int unopt_int(int bit_position)
{
    switch (bit_position)
    {
        case (1 << 0):
            return 0;
        case (1 << 1):
            return 1;
        case (1 << 2):
            return 2;
        case (1 << 3):
            return 3;
        case (1 << 4):
            return 4;
        case (1 << 5):
            return 5;
        case (1 << 6):
            return 6;
        default:
            return 0;
    }
}

int unopt_unsigned_int(unsigned int bit_position)
{
    switch (bit_position)
    {
        case (1 << 0):
            return 0;
        case (1 << 1):
            return 1;
        case (1 << 2):
            return 2;
        case (1 << 3):
            return 3;
        case (1 << 4):
            return 4;
        case (1 << 5):
            return 5;
        case (1 << 6):
            return 6;
        default:
            return 0;
    }
}

int unopt_long(long bit_position)
{
    switch (bit_position)
    {
        case (1 << 0):
            return 0;
        case (1 << 1):
            return 1;
        case (1 << 2):
            return 2;
        case (1 << 3):
            return 3;
        case (1 << 4):
            return 4;
        case (1 << 5):
            return 5;
        case (1 << 6):
            return 6;
        default:
            return 0;
    }
}

int unopt_unsigned_long(unsigned long bit_position)
{
    switch (bit_position)
    {
        case (1 << 0):
            return 0;
        case (1 << 1):
            return 1;
        case (1 << 2):
            return 2;
        case (1 << 3):
            return 3;
        case (1 << 4):
            return 4;
        case (1 << 5):
            return 5;
        case (1 << 6):
            return 6;
        default:
            return 0;
    }
}

#ifdef __x86_64__

int unopt_long_long(long long bit_position)
{
    switch (bit_position)
    {
        case (1 << 0):
            return 0;
        case (1 << 1):
            return 1;
        case (1 << 2):
            return 2;
        case (1 << 3):
            return 3;
        case (1 << 4):
            return 4;
        case (1 << 5):
            return 5;
        case (1 << 6):
            return 6;
        default:
            return 0;
    }
}

int unopt_unsigned_long_long(unsigned long long bit_position)
{
    switch (bit_position)
    {
        case (1 << 0):
            return 0;
        case (1 << 1):
            return 1;
        case (1 << 2):
            return 2;
        case (1 << 3):
            return 3;
        case (1 << 4):
            return 4;
        case (1 << 5):
            return 5;
        case (1 << 6):
            return 6;
        default:
            return 0;
    }
}

#endif

/* { dg-final { scan-tree-dump-times "Applying exponential index transform" 8 "switchconv" { target ia32 } } } */
/* { dg-final { scan-tree-dump-times "Applying exponential index transform" 10 "switchconv" { target { ! ia32 } } } } */
