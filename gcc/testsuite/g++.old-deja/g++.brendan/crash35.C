// Build don't link: 
// GROUPS passed old-abort
const unsigned int ENET_INTERRUPT_MID = 5;

extern "C" void ENET_RxP();

class EnetReceiver
    {
    public:
        friend void ENET_RxP();
        void  receiveMessage();
        int *messagePointer;
    };

void EnetReceiver::receiveMessage()
    {
    if (*((unsigned int*) messagePointer) == ENET_INTERRUPT_MID)
        {
        }
    }
