// { dg-do compile  }
// { dg-options "-O2" }
int * f(void);
void g(int*);
bool h(void);
void Find( )
{
    int * pRes = f();
    if( !pRes )  {
        if( h()){
          if( h()){
            try     
             {       
                pRes = new int();
                f();    
             }catch(int& e1 ){}    
          }     
          if( !pRes )
            f();    
        }
        g(pRes);
    }
}

