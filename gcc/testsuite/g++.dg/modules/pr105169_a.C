/* { dg-module-do link { target { ! *-*-darwin* } } } */
/* { dg-options "-std=c++11 -fpatchable-function-entry=2 -O2" } */
/* { dg-additional-options "-std=c++11 -fpatchable-function-entry=2 -O2" } */

/* This test is in the "modules" package because it supports multiple files
   linkage.  */

#include "pr105169.h"

WinsockInterfaceClass* PacketTransport;

IPXAddressClass::IPXAddressClass(void)
{
}

int function()
{
  return PacketTransport->Get_Protocol();
}

int main()
{
  IPXAddressClass ipxaddr;
  return 0;
}
