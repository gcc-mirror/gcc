CXXFLAGS=--std=c++17 -fcontract-continuation-mode=on
LDFLAGS=-ldl

default: assert_fail libhandle_contract_violation.so libnocontinue.so

run: default
	./assert_fail

runno: default
	LD_PRELOAD="./libnocontinue.so ./libhandle_contract_violation.so" ./assert_fail

runnostd: default
	LD_PRELOAD=./libnocontinue.so ./assert_fail

./libhandle_contract_violation.so: ./handle_contract_violation.cpp
	${CXX} ${CXXFLAGS} -shared -fPIC -o $@ $<

./libnocontinue.so: ./nocontinue.cpp
	${CXX} ${CXXFLAGS} -shared -fPIC -o $@ $<

clean:
	rm -fr ./libhandle_contract_violation.so ./libnocontinue.so ./assert_fail

