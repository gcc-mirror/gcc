unsigned char foo(unsigned long);
int
main(void)
{
    unsigned char AChar;
    unsigned long ALong = 0x12345678;

    AChar = foo(ALong);

    __builtin_printf("AChar = %x\n",(int)AChar);
}
unsigned char
foo( unsigned long TheLong)
{
     return( (unsigned char) (TheLong & 0xff) );
}
