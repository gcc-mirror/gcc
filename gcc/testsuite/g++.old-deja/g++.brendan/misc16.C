// Build don't link: 
// GROUPS passed miscellaneous-bugs
// Using a typedef causes a compiler error
typedef unsigned int Uint32;

// Using a define so that there isn't a typedef works OK.
//#define Uint32 unsigned int

Uint32 func0(Uint32, Uint32)
{
   return 0;
}

Uint32 func1(Uint32, Uint32)
{
   return 1;
}

Uint32 (*mf[])(Uint32, Uint32) = {func0, func1};
