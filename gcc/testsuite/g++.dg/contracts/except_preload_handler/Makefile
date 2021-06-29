CXXFLAGS=--std=c++17 -fcontract-continuation-mode=on

default: assert_fail libhandle_contract_violation.so

run: default
	LD_PRELOAD=./libhandle_contract_violation.so ./assert_fail

./libhandle_contract_violation.so: ./handle_contract_violation.cpp
	${CXX} ${CXXFLAGS} -shared -fPIC -o $@ $<

clean:
	rm -fr ./*.o ./libhandle_contract_violation.so ./assert_fail

