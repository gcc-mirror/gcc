// { dg-do assemble  }
class string
{
public:
    string();
    string(const string& x);
    string(const char* t);

    ~string();
};

void set_status(string message);

class StatusDelay {
private:
    string cause;

public:
    StatusDelay(const string& c)
        : cause(c)
    {
        set_status(cause);
    }

    ~StatusDelay()
    {
        set_status(cause);
    }
};

static char delay_message[] = "Filtering files";

static void searchRemote()
{
    StatusDelay delay(delay_message);
    return;
}
