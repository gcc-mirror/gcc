// { dg-do assemble  }
// 981203 bkoz
// g++/15800  - test

struct panama {
  panama();
  panama(panama &);
  panama& operator=(panama&); // { dg-message "operator=|no known conversion" }
};

extern panama dig();

void foo() {
   panama obj;
   obj = dig(); // { dg-error "rvalue" }
}

