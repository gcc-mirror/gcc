// prms-id: 8155

int fail = 1;

class CMainWindow;
class CFrameWnd;
class CWnd;
class CCmdTarget;
 
typedef void (CCmdTarget::*AFX_PMSG)( void);
typedef void (CWnd::*AFX_PMSGW)( void);

struct AFX_MSGMAP_ENTRY {
  unsigned int  nMessage;    
  AFX_PMSG pfn;
};

struct AFX_MSGMAP {
  const AFX_MSGMAP* pBaseMap;
  const AFX_MSGMAP_ENTRY* lpEntries;
};

class CCmdTarget {
public:
  CCmdTarget();
private:
  static AFX_MSGMAP_ENTRY _messageEntries[];
protected:
  static const AFX_MSGMAP messageMap;
  virtual const AFX_MSGMAP* GetMessageMap() const; 
};

const   AFX_MSGMAP CCmdTarget::messageMap = {
  0, &CCmdTarget::_messageEntries[0]
};

const AFX_MSGMAP* CCmdTarget::GetMessageMap() const {
  return &CCmdTarget::messageMap;
}

AFX_MSGMAP_ENTRY CCmdTarget::_messageEntries[] =
{
  { 0, 0 }
};

CCmdTarget :: CCmdTarget() { }
 
class CWnd : public CCmdTarget {
public:
  CWnd();

protected:
  void OnPaint();
private:
  static AFX_MSGMAP_ENTRY _messageEntries[];
protected:
  static   const AFX_MSGMAP messageMap;
  virtual const AFX_MSGMAP* GetMessageMap() const; 
};

CWnd :: CWnd() {
}

void CWnd :: OnPaint() {
}

const AFX_MSGMAP*   CWnd ::GetMessageMap() const {
  return &  CWnd ::messageMap;
}
const AFX_MSGMAP   CWnd ::messageMap = {
  &  CCmdTarget ::messageMap, &  CWnd ::_messageEntries[0]
  };
AFX_MSGMAP_ENTRY   CWnd ::_messageEntries[] = { 
  {0, (AFX_PMSG)0 } }; 

class CFrameWnd : public CWnd {
public:
  CFrameWnd();
protected:
private:
  static AFX_MSGMAP_ENTRY _messageEntries[];
protected:
  static   const AFX_MSGMAP messageMap;
  virtual const AFX_MSGMAP* GetMessageMap() const; 
};

CFrameWnd :: CFrameWnd() { }

const AFX_MSGMAP*   CFrameWnd ::GetMessageMap() const {
  return &  CFrameWnd ::messageMap;
}
const AFX_MSGMAP   CFrameWnd ::messageMap = {
  &  CWnd ::messageMap, &  CFrameWnd ::_messageEntries[0]
  };
AFX_MSGMAP_ENTRY   CFrameWnd ::_messageEntries[] = { 
  {0, (AFX_PMSG)0 } }; 

class CMainWindow : public CFrameWnd {
public:
  CMainWindow();
  void OnPaint();
  void callProc();
private:
  static AFX_MSGMAP_ENTRY _messageEntries[];
protected:
  static   const AFX_MSGMAP messageMap;
  virtual const AFX_MSGMAP* GetMessageMap() const; 
};

CMainWindow :: CMainWindow()
{
}
void CMainWindow :: OnPaint()
{
  fail = 0;
}

void CMainWindow :: callProc()
{
  const AFX_MSGMAP* pMessageMap;
  const AFX_MSGMAP_ENTRY *lpEntry;

  pMessageMap = GetMessageMap();
  lpEntry = pMessageMap->lpEntries;

  if( lpEntry->nMessage == 100) {
    (this->*lpEntry->pfn)();
  }
}

const AFX_MSGMAP*   CMainWindow ::GetMessageMap() const {
  return &  CMainWindow ::messageMap;
}
const AFX_MSGMAP   CMainWindow ::messageMap = {
  &  CFrameWnd ::messageMap, &  CMainWindow ::_messageEntries[0]
  };
AFX_MSGMAP_ENTRY   CMainWindow ::_messageEntries[] = { 
  { 100, (AFX_PMSG)(AFX_PMSGW)(void (CWnd::*)(void))&CMainWindow::OnPaint },
  {0, (AFX_PMSG)0 }
}; 

int main( int argc, char **argv) {
  CMainWindow     myWindow;

  myWindow.callProc();
  return fail;
}
