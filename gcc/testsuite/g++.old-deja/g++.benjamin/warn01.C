// Build don't link: 
// Special g++ Options:  -Wall -Weffc++

//1 g++/12952 un-named variables in a catch block
//Wall or Wunused should not give warnings here
template <class T>
void f (void) { 
   try
    {
    }
 
   catch( int)
    {    
    }
};

//
//2 g++/12923 __attribute__((__unused__)) not working for objects
//Weffc++ or Wunused should not report the object as an error
class C {
  public:
  C();
};

void f (void){
  C x __attribute__ ((__unused__));
  int y __attribute__ ((__unused__));
}

//
//3 g++/12982 lock should not give error here, as above
void setLock ();
void clearLock ();

template <class T>
class test {
public:
   class lock
   {
   public:
     lock () { setLock(); }
     ~lock () { clearLock(); }
   };

  static void f (void)
  {
   lock local  __attribute__ ((__unused__));
  } 

};


//
//4 g++/12988 neither Mutex nor AutoMutex varibles should give warnings here
//compile with -Weffc++ or -Wunused depending on post or pre 97r1
class Mutex {
private:
  long counter;
public:
  virtual long retcntr() {return counter;};
  Mutex(int i = 0): counter(i) {};
  virtual ~Mutex() {};
} __attribute__ ((__unused__));

class AutoMutex: public Mutex{
private:
  long counter2;
public:
  long retcntr() {return counter2;};
  AutoMutex(int i = 0): counter2(i) {};
  virtual ~AutoMutex() {};
} __attribute__ ((__unused__));


template <class T>
int foofunc(T x){
  Mutex sm(2);
  AutoMutex m(&sm);
  return 0;
};


//5 sanity check to make sure other attributes cannot be used
class Mutex2 {
private:
  long counter;
public:
  virtual long retcntr() {return counter;};
  Mutex2(int i = 0): counter(i) {};
  virtual ~Mutex2() {};
} __attribute__ ((warn));  // WARNING - 







