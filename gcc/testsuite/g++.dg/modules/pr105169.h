class IPXAddressClass
{
public:
    IPXAddressClass(void);
};

class WinsockInterfaceClass
{

public:
    WinsockInterfaceClass(void);

    virtual void Set_Broadcast_Address(void*){};

    virtual int Get_Protocol(void)
    {
        return 0;
    };

protected:
};

