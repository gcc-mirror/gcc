// Build don't link: 
// GROUPS passed old-abort
class memo{
public:
	static int rep;
};

class port_head : public memo {
public:
	static 	int rep;
	unsigned cap();
};

class buff_head : private port_head {
public:
	static int rep;
	port_head::cap;
};
