// 981203 bkoz
// g++/15800 + test
// Build don't link:

struct panama {
  panama();
  panama(panama &);
  panama& operator=(panama&); 
  panama& getref() { return *this; }
};

extern panama dig();

void foo() {
   panama obj;
   obj = dig().getref();
}

