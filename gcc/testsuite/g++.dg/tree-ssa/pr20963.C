/* { dg-do compile } */
/* { dg-options "-O2" } */

/* This was causing PRE to insert the value of the ADDR variable, to 
   remove the invariant cast but in doing so, it was creating a 
   non-invariant expression out of the invariant one, causing a later 
   failure in PRE.  */ 
struct sMCB { 
  unsigned char type; 
}; 
 
extern void foo (void); 
unsigned char mem_readb(char *pt) __attribute__((nothrow)); 
 
void DOS_FreeProcessMemory(unsigned short pspseg) { 
        while (1) { 
                if (pspseg) 
                        foo (); 
                char *addr = (char*)(&((sMCB*)0)->type); 
                if (mem_readb(addr)==0x5a) break; 
        } 
}; 
 
