// { dg-do assemble  }
// prms-id: 7626

int fail;

typedef unsigned int UINT;

class CObject{};

class CCmdTarget : public CObject {
};

typedef void (CCmdTarget::*AFX_PMSG)(void);

struct AFX_MSGMAP_ENTRY {
  AFX_PMSG pfn;     
};

class CWnd : public CCmdTarget {
public:
  void OnMyMsg() { fail  = 1; }		// If this one is called, something is wrong.
  static AFX_MSGMAP_ENTRY _messageEntries[];
};

typedef void (CWnd::*AFX_PMSGW)(void);

class CDialog : public CWnd
{
public:
  void OnMyMsg() { }
  static AFX_MSGMAP_ENTRY _messageEntries[];
};

AFX_MSGMAP_ENTRY  CDialog ::_messageEntries[] = { 
  { (AFX_PMSG)(AFX_PMSGW)(void (CWnd::*)())&CDialog::OnMyMsg }, 
  { (AFX_PMSG)0 }
}; 

int main() {
  CDialog d;
  (d.*((CDialog::_messageEntries)[0]).pfn)();	// This should call CDialog::OnMyMsg
  return fail;
}
