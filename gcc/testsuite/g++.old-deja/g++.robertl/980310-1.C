// { dg-do run  }
// { dg-options "-g" }
// Internal compiler error on egcs 1.0.1 i586-pc-linux-gnulibc1. 
// From: Max Lawson <mlawson@drfmc.ceng.cea.fr>
// Message-Id: <9803091022.AA07520@drfmc.ceng.cea.fr>


typedef __SIZE_TYPE__ size_t;

struct dummy { };

struct arrrrrgh { };

template<class Par,class Rand = arrrrrgh>
struct whyyyyyyy { };	 

template<class T, class S =dummy> 
struct grrrrrrrr { };         

template<class Par, class Par2 =Par, class Rand =arrrrrgh>
class no_future
{
public:
   
  template<class S>
  no_future(const grrrrrrrr<whyyyyyyy<Par,Rand>*,S>& man )  { }

  ~no_future( ) { }

private:

  no_future(const no_future&);
  no_future& operator=(const no_future&);
};	 

int main( )
{
  grrrrrrrr<whyyyyyyy<double>*> man;

  no_future<double> here(man);

  return 0;
}

