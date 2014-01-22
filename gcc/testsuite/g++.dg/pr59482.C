/* { dg-do compile } */
class aa { 
    friend class cc; 
    class bb {}; 
}; 

class cc : aa::bb {};
