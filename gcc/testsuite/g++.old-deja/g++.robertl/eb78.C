// Special g++ Options: -W -Wall -O

//This is the source code from FAQ-259, found in chapter 20 of "C++ FAQs."
//Copyright (C) 1994, Addison-Wesley Publishers, Inc.; All rights reserved.
//
//The book, "C++ FAQs" is by Marshall P. Cline and Greg A. Lomow,
//Copyright (C) 1994, Addison-Wesley Publishers, Inc.; All rights reserved.
//
//This code is presented for its instructional value.  It has been tested with
//care, but it is not guaranteed for any particular purpose.  Neither the
//publisher nor the authors offer any warranties or representations, nor do
//they accept any liabilities with respect to this code.

#include <string.h>
#include <iostream.h>

class BadIndex { };

class String {
public:

  String()
    : len_(0), data_(new char[1])
    { data_[0] = '\0'; }

  String(const char* s)
    : len_(strlen(s)), data_(new char[len_ + 1])
    { memcpy(data_, s, len_ + 1); }

 ~String()
    { delete [] data_; }

  String(const String& s)
    : len_(s.len_), data_(new char[s.len_ + 1])
    { memcpy(data_, s.data_, len_ + 1); }

  String& operator= (const String& s)
    {
      if (len_ != s.len_) { //makes self-assignment harmless
        char* newData = new char[s.len_ + 1];
        delete [] data_;
        data_ = newData;
        len_ = s.len_;
      }
      memcpy(data_, s.data_, len_ + 1);
      return *this;
    }

  unsigned len() const
    { return len_; }

  char& operator[] (unsigned i)
    { indexTest(i); return data_[i]; }
  char  operator[] (unsigned i) const
    { indexTest(i); return data_[i]; }

  friend ostream& operator<< (ostream& o, const String& s)
    { return o.write(s.data_, s.len_); }

  friend int operator== (const String& a, const String& b)
    { return a.len_ == b.len_ &&
             memcmp(a.data_, b.data_, a.len_) == 0; }
  friend int operator!= (const String& a, const String& b)
    { return ! (a == b); }

private:
  void indexTest(unsigned i) const
    { if (i >= len_) throw BadIndex(); }
  unsigned len_;  //ORDER DEPENDENCY; see FAQ-190
  char* data_;    //ORDER DEPENDENCY; see FAQ-190
};

class AccessViolation { };
class BadFileName     { };

class File {
public:
  File(const String& filename)
    throw(AccessViolation, BadFileName)
    {
      cout << "Open " << filename << "\n";
      if (filename == "badAccess.txt")
        throw AccessViolation();
      if (filename == "badName.txt")
        throw BadFileName();
    }
};

class UserClass {
public:
  void f(const String& filename) throw(BadFileName);
};

void
UserClass::f(const String& filename) throw(BadFileName)
{
  try {
    File f(filename);		// WARNING - unused
  }
  catch (const AccessViolation& e) {
    cout << "  FULLY recover from access-violation\n";
  }
  catch (const BadFileName& e) {
    cout << "  PARTIALLY recover from bad-file-name\n";
    throw;
  }
}

void
tryIt(const String& filename)
{
  try {
    UserClass u;
    u.f(filename);
    cout << "  OK\n";
  }
  catch (const BadFileName& e) {
    cout << "  Finish recovering from bad-file-name\n";
  }
}

int
main()
{
  tryIt("goodFile.txt");
  tryIt("badAccess.txt");
  tryIt("badName.txt");
}

// g++ -O -o warn warn.C -W -Wall
// warn.C: In method `void UserClass::f(const class String &)':
// warn.C:96: warning: unused variable `class File f'
// warn.C:101: warning: `struct cp_eh_info * __exception_info' might 
// be used uninitialized in this function


