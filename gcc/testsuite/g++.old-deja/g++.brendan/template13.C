// Build don't link: 
// GROUPS passed templates
template<class T>
class ListS {
public:
    class Vix {
    public:
	Vix();
    };
};

template<class T>
ListS<T>::Vix::Vix()
{ }

