/* { dg-do compile { target { { x86_64-*-* aarch64-*-* ia64-*-* powerpc64-*-* } && lp64 } } } */
/* { dg-options "-O2 -fdump-tree-switchlower1" } */

int f0();    
int f1();    
int f2();    
int f3();    
int f4();    
     
int foo(int a)    
{    
    switch (a)    
    {    
        case 0:    
        case 2:    
        case 4:    
        case 6:    
            return f0();    
        case 8:    
            return f1();    
        case 10:    
        case 14:    
        case 16:    
        case 18:    
            return f2();    
        case 12:    
            return f3();    
        case 20:    
            return f4();    
    }    
    return -1;    
}

/* { dg-final { scan-tree-dump ";; GIMPLE switch case clusters: BT:0-8 BT:10-20" "switchlower1" } } */

int bar(int a)    
{    
    switch (a)    
    {    
        case 20:    
        case 18:    
        case 16:    
        case 14:    
            return f0();    
        case 12:    
            return f1();    
        case 10:    
        case 6:    
        case 4:    
        case 2:    
            return f2();    
        case 8:    
            return f3();    
        case 0:    
            return f4();    
    }    
    return -1;    
}

/* { dg-final { scan-tree-dump ";; GIMPLE switch case clusters: BT:0-10 BT:12-20" "switchlower1" } } */
