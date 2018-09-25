/* { dg-options "-O2 -fdump-tree-switchlower1" } */

int cipher_to_alg(int cipher)        
{                                    
  switch (cipher)              
    {                            
      case 8:   return 2;  
      case 16:  return 3;  
      case 32:  return 4;  
      case 64:  return 6;  
      case 256: return 9;  
      case 512: return 10; 
      case 2048: return 11;
      case 4096: return 12;
      case 8192: return 13;
    }                            
  return 0;                    
}     

/* { dg-final { scan-tree-dump-times "if \\(cipher\[^\n ]*" 12 "switchlower1" } } */
