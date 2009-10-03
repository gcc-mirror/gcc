class base
{
 public:
 base() {}
 virtual ~base() {}
 static base *factory (void);
};

class object : public base
{
 public:
 object() {}
 object (int);
 virtual void key_method (void);
};
