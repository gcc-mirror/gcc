// { dg-do assemble  }
// prms-id: 5793

class temp_string {
 public:
  temp_string (const int);
  temp_string (const char * const);
};

class Range { 
 public:
  Range ( const int);
  operator int () const ;
};

int operator == (const int, temp_string );

void CheckArrayConstraints(void)    { 
  if (Range(0L) == 0L)
    ;
}
