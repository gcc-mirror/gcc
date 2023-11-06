int lwidth;                                                                   
int lheight;                                                                  
int FindNearestPowerOf2 (int);
void ConvertFor3dDriver (int requirePO2, int maxAspect)       
{                                                     
  int oldw = lwidth, oldh = lheight;                      

  lheight = FindNearestPowerOf2 (lheight);            
  while (lwidth/lheight > maxAspect) lheight += lheight;              
}                                                                         
