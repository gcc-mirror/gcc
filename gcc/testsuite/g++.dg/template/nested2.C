template <class T> class CO {
    class CI1 {
	class CI2;
    };
};

template <class T>
class CO<T>::CI1::CI2 {};

