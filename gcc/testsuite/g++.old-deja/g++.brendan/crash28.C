// Build don't link: 
// GROUPS passed old-abort
    class abc
    {
     public: 
      void F()       { return; }

     private:
      typedef int myint;
      typedef struct { int b; } mystruct;
      typedef union  { int c; } myunion;
    };
