
class basic_ios
{
public:
  virtual ~basic_ios();
  basic_ios();
};

class basic_ostream : virtual public basic_ios
{
public:
  basic_ostream();
  virtual ~basic_ostream();
};
