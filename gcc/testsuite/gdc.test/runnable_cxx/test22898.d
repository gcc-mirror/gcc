// EXTRA_CPP_SOURCES: test22898.cpp

import core.stdc.config;

extern(C++):

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Darwin) { /* signed on ARM too */ } else
{
    version (AArch64) version = UnsignedChar;
    version (ARM)     version = UnsignedChar;
    version (RISCV32) version = UnsignedChar;
    version (RISCV64) version = UnsignedChar;
    version (PPC)     version = UnsignedChar;
    version (PPC64)   version = UnsignedChar;
    version (S390)    version = UnsignedChar;
    version (SystemZ) version = UnsignedChar;
}

version (UnsignedChar)
    enum __c_char : ubyte;
else
    enum __c_char : byte;

int testCppCMangle (cpp_ulonglong, __c_char);

void main()
{
    auto val = cast(cpp_ulonglong)18446744073709551488UL;
    auto ch = cast(__c_char)val;
    assert(testCppCMangle(val, ch) == cast(int)ch);
}
