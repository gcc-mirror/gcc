module imports.art4769a;

import core.stdc.stdio;

template DataStreamability(T)
{
     const int isStreamable = true;
     alias T footype;

     void write()
     {
       printf("hallo\n");
     }
}

