// PR tree-optimization/47420
// Testcase by Yu Simin <silver24k@gmail.com>

// { dg-do compile }
// { dg-options "-O2" }

class fooControlBase
{
public:
    fooControlBase() { }

    virtual ~fooControlBase();
};

class fooControl : public fooControlBase
{
public:
    fooControl() { }
};

class sfTextEntryBase
{
public:
    sfTextEntryBase() {  }
    virtual ~sfTextEntryBase();
};

class sfTextEntry : public sfTextEntryBase
{
public:
    sfTextEntry()
    {
    }
};

class sfTextAreaBase
{
public:
    sfTextAreaBase() { }
    virtual ~sfTextAreaBase() { }

protected:
};


class sfTextCtrlBase : public fooControl,
                                   public sfTextAreaBase,
                                   public sfTextEntry
{
public:



    sfTextCtrlBase() { }
    virtual ~sfTextCtrlBase() { }
};

class sfTextCtrl : public sfTextCtrlBase
{
public:
    sfTextCtrl(void* parent)
    {
        Create(parent);
    }
    virtual ~sfTextCtrl();

    bool Create(void *parent);


};

sfTextCtrl* CreateTextCtrl()
{
    return new sfTextCtrl(0);
}

void foo ()
{
    new sfTextCtrl(0);
}
