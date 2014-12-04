factorial: toyvm
	./toyvm factorial.toy 10

fibonacci: toyvm
	./toyvm fibonacci.toy 8

toyvm: toyvm.c Makefile
	g++ -Wall -g -o $@ $< $(shell pkg-config --cflags --libs libgccjit)

clean:
	rm -f *.o toyvm
