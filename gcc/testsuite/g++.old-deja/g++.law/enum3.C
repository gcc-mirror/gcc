// { dg-do assemble  }
// GROUPS passed enums
// enum file
// From: frode@auticon.no
// Date:     Wed, 13 Jan 93 9:24:50 PST
// Subject:  enum trouble
// Message-ID: <"nac.no.001:13.00.93.18.40.52"@nac.no>

typedef enum{on, off} TOGGLE;

class field {
private:
  TOGGLE toggle;
public:
  virtual void on(void) { toggle = 3; };// { dg-error "" } .*
  virtual void off(void) { toggle = on; };// { dg-error "" } .*
};

int main()
{
}
